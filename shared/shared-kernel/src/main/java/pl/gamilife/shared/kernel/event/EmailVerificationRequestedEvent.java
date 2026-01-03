package pl.gamilife.shared.kernel.event;

import java.util.UUID;

public record EmailVerificationRequestedEvent(UUID userId, String verificationCode) {
}
