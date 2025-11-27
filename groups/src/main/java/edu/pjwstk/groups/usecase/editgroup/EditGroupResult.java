package edu.pjwstk.groups.usecase.editgroup;

import lombok.Builder;

import java.io.Serializable;
import java.util.UUID;

@Builder
public record EditGroupResult(UUID groupId, String joinCode, String groupName, UUID adminId,
                              Character groupCurrencySymbol,
                              Integer membersLimit, EditGroupResult.GroupTypeDto groupType)
        implements Serializable {
    /**
     * DTO for {@link edu.pjwstk.groups.model.GroupType}
     */
    public record GroupTypeDto(Integer groupTypeId, String title) implements Serializable {
    }
}

