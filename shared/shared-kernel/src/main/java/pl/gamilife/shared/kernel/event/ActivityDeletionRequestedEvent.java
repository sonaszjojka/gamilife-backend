package pl.gamilife.shared.kernel.event;

import pl.gamilife.shared.kernel.enums.ActivityType;

import java.util.UUID;

public record ActivityDeletionRequestedEvent(
        ActivityType activityType,
        UUID activityId
) {
}
