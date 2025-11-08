package edu.pjwstk.groups.usecase.getgroups;

import java.io.Serializable;
import java.util.UUID;

/**
 * DTO for {@link edu.pjwstk.groups.model.Group}
 */
public record GroupDto(UUID groupId, String joinCode, String groupName, UUID adminId, Character groupCurrencySymbol,
                       Integer membersLimit, GroupTypeDto groupType, Integer membersCount) implements Serializable {
    /**
     * DTO for {@link edu.pjwstk.groups.model.GroupType}
     */
    public record GroupTypeDto(String title) implements Serializable {
    }
}