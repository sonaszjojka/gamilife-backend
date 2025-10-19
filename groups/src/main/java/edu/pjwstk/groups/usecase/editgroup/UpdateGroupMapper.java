package edu.pjwstk.groups.usecase.editgroup;

import edu.pjwstk.groups.entity.Group;

public interface UpdateGroupMapper {
    UpdateGroupResponse toResponse(Group savedGroup);
}
