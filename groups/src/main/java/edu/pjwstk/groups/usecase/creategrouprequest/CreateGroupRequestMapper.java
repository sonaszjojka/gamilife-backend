package edu.pjwstk.groups.usecase.creategrouprequest;

import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupRequest;
import edu.pjwstk.groups.entity.GroupRequestStatus;

import java.util.UUID;

public interface CreateGroupRequestMapper {
    GroupRequest toEntity(UUID groupRequestId, Group group, GroupRequestStatus groupRequestStatus, UUID userId);

    CreateGroupRequestResponse toResponse(GroupRequest savedGroupRequest);
}
