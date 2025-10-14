package edu.pjwstk.groups.usecase.updategroup;

import edu.pjwstk.groups.domain.Group;

public interface UpdateGroupMapper {
    UpdateGroupResponse toResponse(Group savedGroup);
}
