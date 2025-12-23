package pl.gamilife.shared.kernel.event;

import java.util.UUID;

public record TimezoneDetectedEvent(
        UUID userId,
        String timezone
) {
}
