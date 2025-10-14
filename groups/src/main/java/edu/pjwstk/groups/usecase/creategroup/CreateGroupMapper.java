package edu.pjwstk.groups.usecase.creategroup;

import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupType;

import java.util.UUID;

public interface CreateGroupMapper {
    Group toEntity(CreateGroupRequest request, String joinCode, UUID uuid, GroupType groupType);

    CreateGroupResponse toResponse(Group savedGroup);
}
