package edu.pjwstk.groups.usecase.creategroup;

import lombok.Builder;

import java.io.Serializable;
import java.time.Instant;
import java.util.List;
import java.util.UUID;

/**
 * DTO for {@link edu.pjwstk.groups.domain.Group}
 */
@Builder
public record CreateGroupResponse(UUID groupId, String joinCode, UUID adminId, Character groupCurrencySymbol,
                                  Integer membersLimit, GroupTypeDto groupType)
        implements Serializable {
    /**
     * DTO for {@link edu.pjwstk.groups.domain.GroupType}
     */
    public record GroupTypeDto(Integer groupTypeId, String title) implements Serializable {
    }
}