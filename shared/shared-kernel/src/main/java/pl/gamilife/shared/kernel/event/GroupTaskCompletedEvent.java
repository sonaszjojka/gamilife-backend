package pl.gamilife.shared.kernel.event;

import java.util.Collection;
import java.util.UUID;


public record GroupTaskCompletedEvent(
        UUID groupId,
        String groupName,
        UUID groupTaskId,
        String groupTaskTitle,
        Collection<UUID> userIds,
        boolean rewardGranted
) {

}
