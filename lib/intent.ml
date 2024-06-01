type t =
  | GUILDS
  | GUILD_MEMBERS
  | GUILD_MODERATION
  | GUILD_EMOJIS_AND_STICKERS
  | GUILD_INTEGRATIONS
  | GUILD_WEBHOOKS
  | GUILD_INVITES
  | GUILD_VOICE_STATES
  | GUILD_PRESENCES
  | GUILD_MESSAGES
  | GUILD_MESSAGE_REACTIONS
  | GUILD_MESSAGE_TYPING
  | DIRECT_MESSAGES
  | DIRECT_MESSAGE_REACTIONS
  | DIRECT_MESSAGE_TYPING
  | MESSAGE_CONTENT
  | GUILD_SCHEDULED_EVENTS
  | INTENT_17
  | INTENT_18
  | INTENT_19
  | AUTO_MODERATION_CONFIGURATION
  | AUTO_MODERATION_EXECUTION
[@@deriving enum]

let encode = List.fold_left (fun acc i -> acc lor (1 lsl to_enum i)) 0

let of_string = function
  | "GUILDS" -> GUILDS
  | "GUILD_MEMBERS" -> GUILD_MEMBERS
  | "GUILD_MODERATION" -> GUILD_MODERATION
  | "GUILD_EMOJIS_AND_STICKERS" -> GUILD_EMOJIS_AND_STICKERS
  | "GUILD_INTEGRATIONS" -> GUILD_INTEGRATIONS
  | "GUILD_WEBHOOKS" -> GUILD_WEBHOOKS
  | "GUILD_INVITES" -> GUILD_INVITES
  | "GUILD_VOICE_STATES" -> GUILD_VOICE_STATES
  | "GUILD_PRESENCES" -> GUILD_PRESENCES
  | "GUILD_MESSAGES" -> GUILD_MESSAGES
  | "GUILD_MESSAGE_REACTIONS" -> GUILD_MESSAGE_REACTIONS
  | "GUILD_MESSAGE_TYPING" -> GUILD_MESSAGE_TYPING
  | "DIRECT_MESSAGES" -> DIRECT_MESSAGES
  | "DIRECT_MESSAGE_REACTIONS" -> DIRECT_MESSAGE_REACTIONS
  | "DIRECT_MESSAGE_TYPING" -> DIRECT_MESSAGE_TYPING
  | "MESSAGE_CONTENT" -> MESSAGE_CONTENT
  | "GUILD_SCHEDULED_EVENTS" -> GUILD_SCHEDULED_EVENTS
  | "AUTO_MODERATION_CONFIGURATION" -> AUTO_MODERATION_CONFIGURATION
  | "AUTO_MODERATION_EXECUTION" -> AUTO_MODERATION_EXECUTION
  | _ -> failwith (__FUNCTION__ ^ ": Unknown intent")
;;
