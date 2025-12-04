package pl.gamilife.group.usecase.leavegroup;

import lombok.Builder;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupMember;

import java.io.Serializable;
import java.time.Instant;
import java.util.UUID;

/**
 * DTO for {@link GroupMember}
 */
@Builder
public record LeaveGroupResult(UUID groupMemberId, GroupDto group, UUID userId, Instant joinedAt,
                               Instant leftAt, Integer groupMoney,
                               Integer totalEarnedMoney) implements Serializable {
    /**
     * DTO for {@link Group}
     */
    @Builder
    public record GroupDto(UUID groupId) implements Serializable {
    }
}