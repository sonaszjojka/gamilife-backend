package pl.gamilife.group.usecase.editgroup;

import lombok.Builder;
import pl.gamilife.group.model.GroupType;

import java.io.Serializable;
import java.util.UUID;

@Builder
public record EditGroupResult(UUID groupId, String joinCode, String groupName, UUID adminId,
                              Character groupCurrencySymbol,
                              Integer membersLimit, EditGroupResult.GroupTypeDto groupType)
        implements Serializable {
    /**
     * DTO for {@link GroupType}
     */
    public record GroupTypeDto(Integer groupTypeId, String title) implements Serializable {
    }
}

