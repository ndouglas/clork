#!/usr/bin/env python3
"""
Example: Random Agent for Clork

This script demonstrates how to use the ClorkEnv to run a random exploration agent.
The agent randomly selects from valid actions and tracks exploration statistics.

Usage:
    python example_random_agent.py [--steps N] [--episodes N] [--verbose]
"""

import argparse
import random
from clork_env import ClorkEnv, Action


def run_episode(env: ClorkEnv, max_steps: int, verbose: bool = False) -> dict:
    """
    Run one episode with a random agent.

    Returns:
        Episode statistics including total reward and session stats
    """
    obs = env.reset()
    total_reward = 0.0
    steps = 0

    if verbose:
        print(f"\n{'='*60}")
        print(f"Starting in: {env.room_name}")
        print(f"{'='*60}")

    for step in range(max_steps):
        # Get valid actions
        actions = env.valid_actions()
        if not actions:
            if verbose:
                print("No valid actions available!")
            break

        # Bias toward exploration: prefer movement over other actions
        movement_actions = [a for a in actions if a.verb == "go"]
        if movement_actions and random.random() < 0.6:
            action = random.choice(movement_actions)
        else:
            action = random.choice(actions)

        # Take the action
        obs, reward, done, info = env.step(action)
        total_reward += reward
        steps += 1

        if verbose:
            print(f"\n[{step+1}] {action}")
            if reward != 0:
                print(f"     Reward: {reward:.2f}")
            if info.get("rewards", {}).get("novel_room?"):
                print(f"     NEW ROOM: {env.room_name}")
            if info.get("message"):
                msg = info["message"][:100].replace("\n", " ")
                print(f"     {msg}...")

        if done:
            if verbose:
                print("\nGame Over!")
            break

    # Get final stats
    stats = env.session_stats
    stats["total_reward"] = total_reward
    stats["steps"] = steps

    return stats


def main():
    parser = argparse.ArgumentParser(description="Run a random agent on Clork")
    parser.add_argument("--steps", type=int, default=100, help="Max steps per episode")
    parser.add_argument("--episodes", type=int, default=1, help="Number of episodes")
    parser.add_argument("--verbose", "-v", action="store_true", help="Print actions")
    parser.add_argument("--seed", type=int, default=None, help="Random seed")
    args = parser.parse_args()

    if args.seed is not None:
        random.seed(args.seed)

    print("Clork Random Agent")
    print("==================")
    print(f"Episodes: {args.episodes}")
    print(f"Max steps: {args.steps}")

    all_stats = []

    with ClorkEnv(use_rewards=True) as env:
        for ep in range(args.episodes):
            print(f"\n--- Episode {ep+1}/{args.episodes} ---")

            stats = run_episode(env, args.steps, verbose=args.verbose)
            all_stats.append(stats)

            print(f"\nEpisode {ep+1} Results:")
            print(f"  Steps: {stats['steps']}")
            print(f"  Total Reward: {stats['total_reward']:.2f}")
            print(f"  Rooms Discovered: {stats.get('rooms_discovered', 'N/A')}")
            print(f"  Max Score: {stats.get('max_score', 'N/A')}")
            print(f"  Valid Action Rate: {stats.get('valid_action_rate', 0):.1%}")

    # Summary statistics
    if len(all_stats) > 1:
        print(f"\n{'='*60}")
        print("Summary Across All Episodes")
        print(f"{'='*60}")

        avg_reward = sum(s["total_reward"] for s in all_stats) / len(all_stats)
        avg_rooms = sum(s.get("rooms_discovered", 0) for s in all_stats) / len(all_stats)
        max_score = max(s.get("max_score", 0) for s in all_stats)

        print(f"Average Reward: {avg_reward:.2f}")
        print(f"Average Rooms: {avg_rooms:.1f}")
        print(f"Best Score: {max_score}")


if __name__ == "__main__":
    main()
