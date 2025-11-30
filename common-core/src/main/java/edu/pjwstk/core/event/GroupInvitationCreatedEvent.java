package edu.pjwstk.core.event;

import lombok.Value;

import java.util.UUID;

@Value
public class GroupInvitationCreatedEvent {
    UUID userId;
    String joinCode;
    String invitationLink;
}
