(in-ns 'clork.core)

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
