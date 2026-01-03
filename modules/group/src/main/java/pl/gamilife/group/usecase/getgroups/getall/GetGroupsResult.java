package pl.gamilife.group.usecase.getgroups.getall;

import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupType;

import java.io.Serializable;
import java.util.Collection;
import java.util.UUID;


public record GetGroupsResult(
        long totalElements,
        int totalPages,
        int currentPage,
        int pageSize,
        Collection<GroupDto> content
) implements Serializable {

    /**
     * DTO for {@link Group}
     */
    public record GroupDto(
            UUID groupId,
            String groupName,
            UUID adminId,
            Character groupCurrencySymbol,
            Integer membersLimit,
            GroupTypeDto groupType,
            Integer membersCount
    ) {
    }

    /**
     * DTO for {@link GroupType}
     */
    public record GroupTypeDto(String title) implements Serializable {
    }
}