package edu.pjwstk.groups.usecase.getgrouprequests;

import java.io.Serializable;
import java.time.Instant;
import java.util.Collection;
import java.util.UUID;

public record GetGroupRequestsResult(
        long totalElements,
        int totalPages,
        int currentPage,
        int pageSize,
        Collection<GroupRequestDto> content
) implements Serializable {

    public record GroupRequestDto(
            UUID groupRequestId,
            UUID userId,
            String username,
            UUID groupId,
            Instant createdAt,
            GroupRequestStatusDto groupRequestStatus
    ) implements Serializable {
    }

    public record GroupRequestStatusDto(
            Integer groupRequestStatusId,
            String title
    ) implements Serializable {
    }
}