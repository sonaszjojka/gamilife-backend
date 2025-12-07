package pl.gamilife.shared.kernel.event;

import lombok.Value;

import java.util.UUID;

@Value
public class PasswordResetRequestedEvent {
    UUID userId;
    String resetLink;
}
