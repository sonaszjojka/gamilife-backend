package edu.pjwstk.groups.usecase.editgroup;

import lombok.Builder;

import java.io.Serializable;
import java.util.UUID;

@Builder
public record UpdateGroupResponse(UUID groupId, String joinCode, UUID adminId, Character groupCurrencySymbol,
                                  Integer membersLimit, UpdateGroupResponse.GroupTypeDto groupType)
        implements Serializable {
    /**
     * DTO for {@link edu.pjwstk.groups.entity.GroupType}
     */
    public record GroupTypeDto(Integer groupTypeId, String title) implements Serializable {
    }
}

