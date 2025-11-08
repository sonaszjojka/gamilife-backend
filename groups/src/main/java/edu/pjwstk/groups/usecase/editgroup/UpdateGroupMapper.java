package edu.pjwstk.groups.usecase.editgroup;

import edu.pjwstk.groups.model.Group;

public interface UpdateGroupMapper {
    UpdateGroupResponse toResponse(Group savedGroup);
}
