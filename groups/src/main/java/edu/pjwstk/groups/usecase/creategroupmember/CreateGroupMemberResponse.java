package edu.pjwstk.groups.usecase.creategroupmember;

import lombok.Builder;

import java.io.Serializable;
import java.time.Instant;
import java.util.UUID;

/**
 * DTO for {@link edu.pjwstk.groups.model.GroupMember}
 */
@Builder
public record CreateGroupMemberResponse(UUID groupMemberId, GroupDto memberGroup, UUID userId, Instant joinedAt,
                                        Instant leftAt, Integer groupMoney,
                                        Integer totalEarnedMoney) implements Serializable {
    /**
     * DTO for {@link edu.pjwstk.groups.model.Group}
     */
    @Builder
    public record GroupDto(UUID groupId, UUID adminId) implements Serializable {
    }
}