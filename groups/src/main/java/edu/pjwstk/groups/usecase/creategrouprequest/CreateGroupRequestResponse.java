package edu.pjwstk.groups.usecase.creategrouprequest;

import lombok.Builder;

import java.io.Serializable;
import java.time.Instant;
import java.util.UUID;

/**
 * DTO for {@link edu.pjwstk.groups.entity.GroupRequest}
 */
@Builder
public record CreateGroupRequestResponse(UUID groupRequestId, UUID userId, GroupDto groupRequested, Instant createdAt, GroupRequestStatusDto groupRequestStatus) implements Serializable {
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