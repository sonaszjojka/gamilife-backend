package pl.gamilife.group.usecase.creategrouprequest;

import lombok.Builder;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupRequest;
import pl.gamilife.group.model.GroupRequestStatus;

import java.io.Serializable;
import java.time.Instant;
import java.util.UUID;

/**
 * DTO for {@link GroupRequest}
 */
@Builder
public record CreateGroupRequestResult(UUID groupRequestId, UUID userId, GroupDto groupRequested, Instant createdAt,
                                       GroupRequestStatusDto groupRequestStatus) implements Serializable {
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