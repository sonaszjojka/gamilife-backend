package edu.pjwstk.core.event;

import lombok.Value;

import java.util.UUID;

@Value
public class PasswordResetRequestedEvent {
    UUID userId;
    String resetLink;
}
