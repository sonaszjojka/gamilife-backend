package edu.pjwstk.groups.usecase.creategroup;

import edu.pjwstk.groups.domain.Group;
import edu.pjwstk.groups.domain.GroupType;

import java.util.UUID;

public interface CreateGroupMapper {
    Group toEntity(CreateGroupRequest request, String joinCode, UUID uuid, GroupType groupType);

    CreateGroupResponse toResponse(Group savedGroup);
}
