package pl.gamilife.group.usecase.editgrouprequeststatus;

import lombok.Builder;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupRequestStatus;

import java.io.Serializable;
import java.time.Instant;
import java.util.UUID;

@Builder
public record EditGroupRequestStatusResult(UUID groupRequestId, UUID userId,
                                           EditGroupRequestStatusResult.GroupDto groupRequested,
                                           Instant createdAt,
                                           EditGroupRequestStatusResult.GroupRequestStatusDto groupRequestStatus,
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
