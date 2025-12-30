package pl.gamilife.shared.kernel.event;

import java.util.UUID;

public record GroupItemUsedEvent(
        UUID adminId,
        UUID userId,
        UUID groupItemId,
        String groupItemName
) {
}
