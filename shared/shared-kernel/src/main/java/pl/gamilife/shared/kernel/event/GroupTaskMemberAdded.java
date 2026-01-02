package pl.gamilife.shared.kernel.event;

import java.util.UUID;

public record GroupTaskMemberAdded(
        UUID userId,
        UUID groupTaskId,
        String groupTaskTitle,
        UUID groupId,
        String groupName
) {
}
