package pl.gamilife.shared.kernel.event;

import java.util.UUID;

public record TaskUndoneEvent(
        UUID userId,
        UUID taskId
) {
}

