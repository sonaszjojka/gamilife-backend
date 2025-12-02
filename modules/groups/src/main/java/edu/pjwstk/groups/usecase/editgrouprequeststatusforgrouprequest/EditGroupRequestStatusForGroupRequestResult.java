package edu.pjwstk.groups.usecase.editgrouprequeststatusforgrouprequest;

import lombok.Builder;

import java.io.Serializable;
import java.time.Instant;
import java.util.UUID;

@Builder
public record EditGroupRequestStatusForGroupRequestResult(UUID groupRequestId, UUID userId,
                                                          EditGroupRequestStatusForGroupRequestResult.GroupDto groupRequested,
                                                          Instant createdAt,
                                                          EditGroupRequestStatusForGroupRequestResult.GroupRequestStatusDto groupRequestStatus,
                                                          UUID groupMemberId)
        implements Serializable {
    /**
     * DTO for {@link edu.pjwstk.groups.model.Group}
     */
    @Builder
    public record GroupDto(UUID groupId) implements Serializable {
    }

    /**
     * DTO for {@link edu.pjwstk.groups.model.GroupRequestStatus}
     */
    @Builder
    public record GroupRequestStatusDto(Integer groupRequestStatusId, String title) implements Serializable {
    }
}
