package pl.gamilife.group.usecase.creategroupmemberinopengroup;

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
public record CreateGroupMemberInOpenGroupResult(UUID groupMemberId, GroupDto memberGroup, UUID userId,
                                                 Instant joinedAt,
                                                 Instant leftAt, Integer groupMoney,
                                                 Integer totalEarnedMoney) implements Serializable {
    /**
     * DTO for {@link Group}
     */
    @Builder
    public record GroupDto(UUID groupId, UUID adminId) implements Serializable {
    }
}