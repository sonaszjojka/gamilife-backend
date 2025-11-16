package edu.pjwstk.groups.usecase.getgroups.getbyid;

import java.io.Serializable;
import java.time.Instant;
import java.util.Collection;
import java.util.UUID;


public record GetGroupByIdResult(
        UUID groupId,
        String joinCode,
        String groupName,
        UUID adminId,
        Character groupCurrencySymbol,
        Integer membersLimit,
        GroupTypeDto groupType,
        Integer membersCount,
        Boolean isMember,
        Boolean hasActiveGroupRequest,
        GroupMemberDto loggedUserMembershipDto,
        Collection<GroupMemberDto> members
) implements Serializable {

    public record GroupTypeDto(Integer id, String title) implements Serializable {
    }

    /**
     * DTO for {@link edu.pjwstk.groups.model.GroupMember}
     */
    public record GroupMemberDto(UUID groupMemberId, UUID groupId, UUID userId, Integer groupMoney,
                                 Integer totalEarnedMoney, Instant joinedAt, Instant leftAt, String username) implements Serializable {
    }
}