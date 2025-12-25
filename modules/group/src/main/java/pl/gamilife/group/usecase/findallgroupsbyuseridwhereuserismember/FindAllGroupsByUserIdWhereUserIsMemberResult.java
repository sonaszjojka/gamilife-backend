package pl.gamilife.group.usecase.findallgroupsbyuseridwhereuserismember;

import java.io.Serializable;
import java.util.Collection;
import java.util.UUID;


public record FindAllGroupsByUserIdWhereUserIsMemberResult(
        int totalElements,
        int totalPages,
        int currentPage,
        int pageSize,
        Collection<GroupDto> content
) implements Serializable {

    public record GroupDto(
            UUID groupId,
            String joinCode,
            String groupName,
            UUID adminId,
            Character groupCurrencySymbol,
            Integer membersLimit,
            GroupTypeDto groupType,
            Integer membersCount
    ) {
    }

    public record GroupTypeDto(String title) implements Serializable {
    }
}