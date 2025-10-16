package edu.pjwstk.groups.usecase.editgrouprequeststatusforgrouprequest;

import edu.pjwstk.groups.usecase.creategrouprequest.CreateGroupRequestResponse;
import lombok.Builder;

import java.io.Serializable;
import java.time.Instant;
import java.util.UUID;

@Builder
public record EditGroupRequestStatusForGroupRequestResponse(UUID groupRequestId, UUID userId,
                                                            EditGroupRequestStatusForGroupRequestResponse.GroupDto groupRequested,
                                                            Instant createdAt,
                                                            EditGroupRequestStatusForGroupRequestResponse.GroupRequestStatusDto groupRequestStatus)
        implements Serializable {
    /**
     * DTO for {@link edu.pjwstk.groups.entity.Group}
     */
    @Builder
    public record GroupDto(UUID groupId) implements Serializable {
    }

    /**
     * DTO for {@link edu.pjwstk.groups.entity.GroupRequestStatus}
     */
    @Builder
    public record GroupRequestStatusDto(Integer groupRequestStatusId, String title) implements Serializable {
    }
}
