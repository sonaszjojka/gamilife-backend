package edu.pjwstk.groups.usecase.findgroupbyid;

import lombok.Builder;

import java.io.Serializable;
import java.util.UUID;

/**
 * DTO for {@link edu.pjwstk.groups.entity.Group}
 */
@Builder
public record GroupDto(UUID groupId, String joinCode, UUID adminId, Character groupCurrencySymbol, Integer membersLimit,
                       GroupTypeDto groupType) implements Serializable {
    /**
     * DTO for {@link edu.pjwstk.groups.entity.GroupType}
     */
    @Builder
    public record GroupTypeDto(Integer groupTypeId, String title) implements Serializable {
    }
}