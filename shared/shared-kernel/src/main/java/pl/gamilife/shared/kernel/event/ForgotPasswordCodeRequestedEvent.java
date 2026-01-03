package pl.gamilife.shared.kernel.event;

import java.util.UUID;

public record ForgotPasswordCodeRequestedEvent(UUID userId, String forgotPasswordCode) {
}
