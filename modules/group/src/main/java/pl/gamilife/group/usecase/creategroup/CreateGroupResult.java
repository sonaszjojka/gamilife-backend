package pl.gamilife.group.usecase.creategroup;

import lombok.Builder;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupType;

import java.io.Serializable;
import java.util.UUID;

/**
 * DTO for {@link Group}
 */
@Builder
public record CreateGroupResult(UUID groupId, String groupName, String joinCode, UUID adminId,
                                Character groupCurrencySymbol,
                                Integer membersLimit, GroupTypeDto groupType)
        implements Serializable {
    /**
     * DTO for {@link GroupType}
     */
    public record GroupTypeDto(Integer groupTypeId, String title) implements Serializable {
    }
}