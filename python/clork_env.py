"""
Clork ML Environment - Python wrapper for training ML agents on Clork.

Provides an OpenAI Gym-style interface for interacting with the Clork
text adventure game via structured actions (no text parsing needed).

Usage:
    from clork_env import ClorkEnv

    env = ClorkEnv()
    obs = env.reset()

    while not obs['game-over']:
        # Get valid actions
        actions = env.valid_actions()

        # Pick an action (e.g., random exploration)
        action = random.choice(actions)

        # Take the action
        obs, reward, done, info = env.step(action)

    env.close()
"""

import json
import subprocess
import os
from typing import Dict, List, Any, Optional, Tuple
from dataclasses import dataclass
from pathlib import Path


@dataclass
class Action:
    """Represents a valid action in Clork."""
    verb: str
    direction: Optional[str] = None
    direct_object: Optional[str] = None
    indirect_object: Optional[str] = None
    prep: Optional[str] = None

    def to_dict(self) -> Dict[str, str]:
        """Convert to JSON-serializable dict."""
        d = {"verb": self.verb}
        if self.direction:
            d["direction"] = self.direction
        if self.direct_object:
            d["direct-object"] = self.direct_object
        if self.indirect_object:
            d["indirect-object"] = self.indirect_object
        if self.prep:
            d["prep"] = self.prep
        return d

    @classmethod
    def from_dict(cls, d: Dict[str, Any]) -> "Action":
        """Create from dict."""
        return cls(
            verb=d.get("verb"),
            direction=d.get("direction"),
            direct_object=d.get("direct-object"),
            indirect_object=d.get("indirect-object"),
            prep=d.get("prep"),
        )

    def __str__(self) -> str:
        """Human-readable representation."""
        parts = [self.verb]
        if self.direction:
            parts.append(self.direction)
        if self.direct_object:
            parts.append(self.direct_object)
        if self.prep and self.indirect_object:
            parts.extend([self.prep, self.indirect_object])
        return " ".join(parts)


class ClorkEnv:
    """
    OpenAI Gym-style environment for Clork.

    The environment communicates with a Clork subprocess via JSON lines,
    providing structured actions and observations without text parsing.

    Attributes:
        observation: Current game state observation
        reward_weights: Custom weights for reward calculation (optional)
    """

    def __init__(
        self,
        clork_dir: Optional[str] = None,
        use_rewards: bool = True,
        reward_weights: Optional[Dict[str, float]] = None,
        java_opts: Optional[List[str]] = None,
    ):
        """
        Initialize the Clork environment.

        Args:
            clork_dir: Path to Clork project directory (default: auto-detect)
            use_rewards: Whether to use reward-tracking mode
            reward_weights: Custom reward weights (see default_reward_weights)
            java_opts: Additional JVM options
        """
        self.clork_dir = Path(clork_dir) if clork_dir else self._find_clork_dir()
        self.use_rewards = use_rewards
        self.reward_weights = reward_weights
        self.java_opts = java_opts or []

        self._process: Optional[subprocess.Popen] = None
        self._observation: Optional[Dict[str, Any]] = None
        self._last_reward: float = 0.0
        self._done: bool = False

    @staticmethod
    def _find_clork_dir() -> Path:
        """Auto-detect Clork directory."""
        # Try relative to this file
        this_file = Path(__file__).resolve()
        clork_dir = this_file.parent.parent

        if (clork_dir / "project.clj").exists():
            return clork_dir

        # Try current working directory
        cwd = Path.cwd()
        if (cwd / "project.clj").exists():
            return cwd

        raise RuntimeError(
            "Could not find Clork directory. "
            "Please specify clork_dir parameter or run from Clork project root."
        )

    @property
    def default_reward_weights(self) -> Dict[str, float]:
        """Default reward weights used by Clork."""
        return {
            "score-delta": 1.0,
            "novel-room": 5.0,
            "novel-message": 0.5,
            "object-taken": 2.0,
            "container-opened": 1.5,
            "death": -10.0,
            "invalid-action": -0.1,
        }

    def _start_process(self):
        """Start the Clork subprocess."""
        if self._process is not None:
            self.close()

        # Build command
        cmd = ["lein", "run", "--ml"]
        if self.use_rewards:
            cmd.append("--ml-rewards")

        env = os.environ.copy()
        if self.java_opts:
            env["JVM_OPTS"] = " ".join(self.java_opts)

        self._process = subprocess.Popen(
            cmd,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            cwd=str(self.clork_dir),
            text=True,
            bufsize=1,  # Line buffered
            env=env,
        )

    def _send_action(self, action: Dict[str, str]) -> Dict[str, Any]:
        """Send an action and receive the response."""
        if self._process is None:
            raise RuntimeError("Environment not started. Call reset() first.")

        # Send action as JSON line
        json_line = json.dumps(action) + "\n"
        self._process.stdin.write(json_line)
        self._process.stdin.flush()

        # Read response
        response_line = self._process.stdout.readline()
        if not response_line:
            # Process may have died
            stderr = self._process.stderr.read()
            raise RuntimeError(f"Clork process died unexpectedly: {stderr}")

        return json.loads(response_line)

    def _read_initial_state(self) -> Dict[str, Any]:
        """Read the initial state after starting the process."""
        if self._process is None:
            raise RuntimeError("Process not started")

        response_line = self._process.stdout.readline()
        if not response_line:
            stderr = self._process.stderr.read()
            raise RuntimeError(f"Failed to read initial state: {stderr}")

        return json.loads(response_line)

    def reset(self) -> Dict[str, Any]:
        """
        Reset the environment to initial state.

        Returns:
            Initial observation dictionary
        """
        if self._process is None:
            self._start_process()
            self._observation = self._read_initial_state()
        else:
            # Send reset command
            response = self._send_action({"verb": "reset"})
            self._observation = response

        self._last_reward = 0.0
        self._done = False

        return self._observation

    def step(self, action: Action | Dict[str, str] | str) -> Tuple[Dict[str, Any], float, bool, Dict[str, Any]]:
        """
        Take an action in the environment.

        Args:
            action: Action to take. Can be:
                - Action object
                - Dict with verb/direction/direct-object keys
                - String shorthand like "north", "take lamp"

        Returns:
            Tuple of (observation, reward, done, info)
            - observation: New game state
            - reward: Reward from this action (composite reward if use_rewards=True)
            - done: Whether the game is over
            - info: Additional information (rewards breakdown, session stats)
        """
        # Convert action to dict
        if isinstance(action, Action):
            action_dict = action.to_dict()
        elif isinstance(action, str):
            action_dict = self._parse_action_string(action)
        else:
            action_dict = action

        # Send action
        response = self._send_action(action_dict)
        self._observation = response

        # Extract reward
        if self.use_rewards and "composite-reward" in response:
            reward = response["composite-reward"]
        elif "rewards" in response and "score-delta" in response["rewards"]:
            reward = response["rewards"]["score-delta"]
        else:
            reward = 0.0

        self._last_reward = reward

        # Check if done
        done = response.get("game-over", False)
        self._done = done

        # Build info dict
        info = {}
        if "rewards" in response:
            info["rewards"] = response["rewards"]
        if "session-stats" in response:
            info["session-stats"] = response["session-stats"]
        if "message" in response:
            info["message"] = response["message"]

        return self._observation, reward, done, info

    def _parse_action_string(self, action_str: str) -> Dict[str, str]:
        """Parse a simple action string into action dict."""
        parts = action_str.lower().split()

        if not parts:
            return {"verb": "look"}

        # Direction shortcuts
        directions = {
            "n", "s", "e", "w", "ne", "nw", "se", "sw",
            "north", "south", "east", "west",
            "northeast", "northwest", "southeast", "southwest",
            "up", "down", "u", "d", "in", "out"
        }

        if parts[0] in directions:
            return {"verb": "go", "direction": parts[0]}

        # Simple verb
        if len(parts) == 1:
            return {"verb": parts[0]}

        # Verb + object
        if len(parts) == 2:
            return {"verb": parts[0], "direct-object": parts[1]}

        # Verb + object + prep + object
        if len(parts) >= 4:
            return {
                "verb": parts[0],
                "direct-object": parts[1],
                "prep": parts[2],
                "indirect-object": parts[3],
            }

        return {"verb": parts[0], "direct-object": " ".join(parts[1:])}

    # Meta verbs that should be excluded from random action selection
    # These either end the game or have side effects unsuitable for training
    EXCLUDED_META_VERBS = {"quit", "save", "restore", "restart"}

    def valid_actions(self, include_dangerous: bool = False) -> List[Action]:
        """
        Get list of valid actions from current state.

        Args:
            include_dangerous: If True, include quit/save/restore/restart actions.
                              Default False to prevent random agents from quitting.

        Returns:
            List of Action objects that can be taken
        """
        if self._observation is None:
            raise RuntimeError("No observation available. Call reset() first.")

        actions = []
        va = self._observation.get("valid-actions", {})

        # Meta verbs (look, inventory, etc.)
        for verb in va.get("meta-verbs", []):
            if include_dangerous or verb not in self.EXCLUDED_META_VERBS:
                actions.append(Action(verb=verb))

        # Movement
        movement = va.get("movement", {})
        for direction in movement.get("directions", []):
            actions.append(Action(verb="go", direction=direction))

        # Object actions
        object_actions = va.get("object-actions", {})
        for obj_id, obj_info in object_actions.items():
            for verb in obj_info.get("verbs", []):
                actions.append(Action(verb=verb, direct_object=obj_id))

        # Two-object actions
        for two_obj in va.get("two-object-actions", []):
            actions.append(Action(
                verb=two_obj.get("verb"),
                direct_object=two_obj.get("direct-object"),
                prep=two_obj.get("prep"),
                indirect_object=two_obj.get("indirect-object"),
            ))

        return actions

    def valid_action_dicts(self) -> List[Dict[str, str]]:
        """Get valid actions as list of dicts (for JSON serialization)."""
        return [a.to_dict() for a in self.valid_actions()]

    @property
    def observation(self) -> Optional[Dict[str, Any]]:
        """Current observation."""
        return self._observation

    @property
    def score(self) -> int:
        """Current game score."""
        if self._observation is None:
            return 0
        return self._observation.get("score", 0)

    @property
    def moves(self) -> int:
        """Number of moves taken."""
        if self._observation is None:
            return 0
        return self._observation.get("moves", 0)

    @property
    def room(self) -> Optional[str]:
        """Current room ID."""
        if self._observation is None:
            return None
        room = self._observation.get("room", {})
        return room.get("id")

    @property
    def room_name(self) -> Optional[str]:
        """Current room name."""
        if self._observation is None:
            return None
        room = self._observation.get("room", {})
        return room.get("name")

    @property
    def message(self) -> str:
        """Last message from the game."""
        if self._observation is None:
            return ""
        return self._observation.get("message", "")

    @property
    def inventory(self) -> List[str]:
        """List of items in inventory."""
        if self._observation is None:
            return []
        va = self._observation.get("valid-actions", {})
        return [item["id"] for item in va.get("inventory", [])]

    @property
    def session_stats(self) -> Dict[str, Any]:
        """Session statistics (if using rewards mode)."""
        if self._observation is None:
            return {}
        return self._observation.get("session-stats", {})

    def get_stats(self) -> Dict[str, Any]:
        """Request current session statistics."""
        if self._process is None:
            return {}
        response = self._send_action({"verb": "stats"})
        return response.get("session-stats", {})

    def close(self):
        """Close the environment and cleanup subprocess."""
        if self._process is not None:
            try:
                # Try graceful shutdown
                self._process.stdin.write(json.dumps({"verb": "quit"}) + "\n")
                self._process.stdin.flush()
                self._process.wait(timeout=2)
            except Exception:
                # Force kill if needed
                self._process.kill()
            finally:
                self._process = None

    def __enter__(self):
        """Context manager entry."""
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit."""
        self.close()

    def __del__(self):
        """Destructor - ensure cleanup."""
        self.close()


class ClorkVecEnv:
    """
    Vectorized environment for parallel training.

    Runs multiple Clork instances in parallel for faster training.
    """

    def __init__(self, num_envs: int = 4, **env_kwargs):
        """
        Initialize vectorized environment.

        Args:
            num_envs: Number of parallel environments
            **env_kwargs: Arguments passed to each ClorkEnv
        """
        self.num_envs = num_envs
        self.envs = [ClorkEnv(**env_kwargs) for _ in range(num_envs)]

    def reset(self) -> List[Dict[str, Any]]:
        """Reset all environments."""
        return [env.reset() for env in self.envs]

    def step(self, actions: List[Any]) -> Tuple[List[Dict], List[float], List[bool], List[Dict]]:
        """
        Take actions in all environments.

        Args:
            actions: List of actions, one per environment

        Returns:
            Tuple of (observations, rewards, dones, infos)
        """
        results = [env.step(action) for env, action in zip(self.envs, actions)]

        observations = [r[0] for r in results]
        rewards = [r[1] for r in results]
        dones = [r[2] for r in results]
        infos = [r[3] for r in results]

        return observations, rewards, dones, infos

    def valid_actions(self) -> List[List[Action]]:
        """Get valid actions for all environments."""
        return [env.valid_actions() for env in self.envs]

    def close(self):
        """Close all environments."""
        for env in self.envs:
            env.close()

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.close()


# Convenience functions for quick testing

def random_agent(env: ClorkEnv, max_steps: int = 100, verbose: bool = True) -> Dict[str, Any]:
    """
    Run a random agent for testing.

    Args:
        env: ClorkEnv instance
        max_steps: Maximum steps to take
        verbose: Whether to print actions and messages

    Returns:
        Final session statistics
    """
    import random

    obs = env.reset()
    total_reward = 0.0

    for step in range(max_steps):
        actions = env.valid_actions()
        if not actions:
            break

        action = random.choice(actions)
        obs, reward, done, info = env.step(action)
        total_reward += reward

        if verbose:
            print(f"[{step}] {action} -> reward={reward:.2f}")
            if info.get("message"):
                print(f"    {info['message'][:80]}...")

        if done:
            break

    stats = env.session_stats
    stats["total_reward"] = total_reward
    return stats


if __name__ == "__main__":
    # Quick test
    print("Testing ClorkEnv...")

    with ClorkEnv() as env:
        obs = env.reset()
        print(f"Starting room: {env.room_name}")
        print(f"Valid actions: {len(env.valid_actions())}")

        # Take a few actions
        for action_str in ["look", "north", "look", "south"]:
            obs, reward, done, info = env.step(action_str)
            print(f"\n> {action_str}")
            print(f"  Room: {env.room_name}")
            print(f"  Reward: {reward}")
            if info.get("message"):
                print(f"  Message: {info['message'][:100]}...")

        print(f"\nSession stats: {env.session_stats}")
