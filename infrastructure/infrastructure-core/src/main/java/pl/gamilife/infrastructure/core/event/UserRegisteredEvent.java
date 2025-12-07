package pl.gamilife.infrastructure.core.event;

import java.util.UUID;

public record UserRegisteredEvent(UUID userId) {
}
