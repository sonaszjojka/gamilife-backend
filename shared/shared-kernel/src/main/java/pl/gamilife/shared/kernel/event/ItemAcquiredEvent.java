package pl.gamilife.shared.kernel.event;

import java.util.Collection;
import java.util.UUID;

public record ItemAcquiredEvent(
        UUID userId,
        Collection<String> itemNames
) {
}
