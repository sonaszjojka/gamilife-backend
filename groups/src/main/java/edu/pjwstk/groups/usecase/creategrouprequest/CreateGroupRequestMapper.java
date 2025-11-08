package edu.pjwstk.groups.usecase.creategrouprequest;

import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.model.GroupRequest;
import edu.pjwstk.groups.model.GroupRequestStatus;

import java.util.UUID;

public interface CreateGroupRequestMapper {
    GroupRequest toEntity(UUID groupRequestId, Group group, GroupRequestStatus groupRequestStatus, UUID userId);

    CreateGroupRequestResponse toResponse(GroupRequest savedGroupRequest);
}
