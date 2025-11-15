package edu.pjwstk.groups.usecase.getgroups.getbyid;

import java.io.Serializable;
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
        UUID groupMemberIdForLoggedUser
) implements Serializable {

    /**
     * DTO for {@link edu.pjwstk.groups.model.GroupType}
     */
    public record GroupTypeDto(String title) implements Serializable {
    }
}