package pl.gamilife.group.usecase.editgrouprequeststatusforgrouprequest;

import lombok.Builder;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupRequestStatus;

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
     * DTO for {@link Group}
     */
    @Builder
    public record GroupDto(UUID groupId) implements Serializable {
    }

    /**
     * DTO for {@link GroupRequestStatus}
     */
    @Builder
    public record GroupRequestStatusDto(Integer groupRequestStatusId, String title) implements Serializable {
    }
}
