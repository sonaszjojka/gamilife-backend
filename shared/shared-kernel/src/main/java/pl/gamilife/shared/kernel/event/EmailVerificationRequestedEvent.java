package pl.gamilife.shared.kernel.event;

import lombok.Value;

import java.util.UUID;

@Value
public class EmailVerificationRequestedEvent {
    UUID userId;
    String verificationLink;
}
