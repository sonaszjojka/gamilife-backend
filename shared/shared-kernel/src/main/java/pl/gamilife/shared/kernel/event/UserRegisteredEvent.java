package pl.gamilife.shared.kernel.event;

import java.util.UUID;

public record UserRegisteredEvent(UUID userId) {
}
