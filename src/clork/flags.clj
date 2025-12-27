(ns clork.flags
  "Flag system for objects and rooms in Clork.")

;; Map of all known flag names to their descriptions
(def flag-names
  {:take      "Player can pick up the object"
   :trytake   "Parser won't implicitly take object"
   :cont      "Object is a container"
   :door      "Object is a door"
   :open      "Door/container is open"
   :surface   "Object is a surface (table, desk)"
   :locked    "Locked, needs equipment to open"
   :wear      "Object is wearable"
   :worn      "Object is being worn"
   :read      "Object is readable (has :text)"
   :light     "Object can provide light"
   :on        "Light source is turned on"
   :flame     "Object is a source of fire"
   :burn      "Object is burnable"
   :trans     "Transparent container"
   :ndesc     "Room description includes this object"
   :invisible "Parser ignores this object"
   :touch     "Room visited / object disturbed"
   :search    "Search deeply in container"
   :vehicle   "Object is a vehicle"
   :person    "Object is a character"
   :female    "Character is female"
   :vowel     "Use 'an' instead of 'a'"
   :narticle  "Omit articles"
   :plural    "Description is plural"
   :rland     "Room is dry land"
   :rwater    "Room is water"
   :rair      "Room is mid-air"
   :kludge    "Syntax file flag"
   :outside   "Room is outdoors"
   :integral  "Part of another object"
   :part      "Object is a body part"
   :nall      "Take all ignores this"
   :drop      "Dropped items stay in vehicle"
   :in        "Say 'in the vehicle' not 'on'"})

(defn get-default-flags
  "Get the default (all-off) flags set."
  []
  (let [default-flags {
    ;; The player can pick up and carry the object.
    :take false,
    ;; Tells the parser not to let the player implicitly take an object.
    :trytake false,
    ;; The object is a container; things can be put inside it, it can be opened
    ;; and closed, etc.
    :cont false,
    ;; The object is a door.
    :door false,
    ;; The object is a door or container, and is open.
    :open false,
    ;; The object is a surface, such as a table, desk, countertop, etc.
    :surface false,
    ;; Locked and can't be opened without proper equipment.
    :locked false,
    ;; The object is wearable (not necessarily being worn).
    :wear false,
    ;; The object is currently being worn.
    :worn false,
    ;; The object is readable (has a :text property).
    :read false,
    ;; The object is capable of being turned on and off.
    :light false,
    ;; The room is lit, or the object is providing light.
    :on false,
    ;; The object is a source of fire.
    :flame false,
    ;; The object is burnable.
    :burn false,
    ;; The object is transparent; objects inside it can be seen even if it is
    ;; closed.
    :trans false,
    ;; The room description is describing this object. Should be cleared once
    ;; taken.
    :ndesc false,
    ;; Tells the parser not to find this object; the bit would presumably be
    ;; cleared at some point.
    :invisible false,
    ;; For rooms, player has been to the room at least once. For objects, it
    ;; has been taken or otherwise disturbed by the player.
    :touch false,
    ;; Tells the parser to look as deeply into a container as it can in order
    ;; to find the referened object.
    :search false,
    ;; The object is a vehicle.
    :vehicle false,
    ;; The object is a character in the game.
    :person false,
    ;; The object is an actor who is a female.
    :female false,
    ;; Any verb default which prints an indefinite article before the :desc,
    ;; use "an" instead of "a".
    :vowel false,
    ;; The object's :desc doesn't work with articles, and they should be
    ;; omitted.
    :narticle false,
    ;; The object's :desc is a plural noun or noun phrase.
    :plural false,
    ;; Indicates that the room is dry land.
    :rland false,
    ;; The room is water rather than dry land.
    :rwater false,
    ;; The room is mid-air.
    :rair false,
    ;; This bit is used only in the syntax file.
    :kludge false,
    ;; This room is outdoors.
    :outside false,
    ;; An integral part of another object; can't be taken or dropped.
    :integral false,
    ;; The object is a body part.
    :part false,
    ;; "Take all" should not take this object.
    :nall false,
    ;; Found in vehicles. If the player drops an item, it stays in the vehicle
    ;; rather than into the room outside.
    :drop false,
    ;; Found in vehicles. Tells routines to say "in the vehicle" instead of "on
    ;; the vehicle."
    :in false,
  }]
  (set (for [[k v] default-flags :when v] k))))

(defn flags
  "Returns a new set of flags with the specified flags set."
  [& set-flags]
  (let [flag-set (get-default-flags)]
    (reduce conj flag-set set-flags)))
