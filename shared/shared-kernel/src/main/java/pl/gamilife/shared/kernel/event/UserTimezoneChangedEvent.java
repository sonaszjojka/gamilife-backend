package pl.gamilife.shared.kernel.event;

import java.util.UUID;

public record UserTimezoneChangedEvent(
        UUID userId,
        String oldTimezone,
        String newTimezone
) {
}
