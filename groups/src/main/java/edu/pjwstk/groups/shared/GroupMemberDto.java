package edu.pjwstk.groups.shared;

import lombok.Builder;

import java.io.Serializable;
import java.time.Instant;
import java.util.UUID;

/**
 * DTO for {@link edu.pjwstk.groups.domain.GroupMember}
 */
@Builder
public record GroupMemberDto(Integer groupMemberId, GroupDto memberGroup, UUID userId, Instant joinedAt, Instant leftAt,
                             Integer groupMoney, Integer totalEarnedMoney) implements Serializable {
    /**
     * DTO for {@link edu.pjwstk.groups.domain.Group}
     */
    public record GroupDto(UUID groupId) implements Serializable {
    }
}