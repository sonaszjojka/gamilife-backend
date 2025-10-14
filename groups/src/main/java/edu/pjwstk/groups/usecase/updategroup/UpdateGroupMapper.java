package edu.pjwstk.groups.usecase.updategroup;

import edu.pjwstk.groups.entity.Group;

public interface UpdateGroupMapper {
    UpdateGroupResponse toResponse(Group savedGroup);
}
