package edu.pjwstk.groups.usecase.editgrouprequeststatusforgrouprequest;

import edu.pjwstk.groups.usecase.creategroupmember.CreateGroupMemberResponse;
import lombok.Builder;

import java.io.Serializable;
import java.time.Instant;
import java.util.UUID;

@Builder
public record EditGroupRequestStatusForGroupRequestResponse(UUID groupRequestId, UUID userId,
                                                            EditGroupRequestStatusForGroupRequestResponse.GroupDto groupRequested,
                                                            Instant createdAt,
                                                            EditGroupRequestStatusForGroupRequestResponse.GroupRequestStatusDto groupRequestStatus,
                                                            CreateGroupMemberResponse groupMember)
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
