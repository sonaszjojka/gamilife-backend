package pl.gamilife.shared.kernel.event;

import java.util.UUID;

public record GroupDeletionRequestedEvent(
        UUID groupId
) {
}
