package edu.pjwstk.groups.usecase.findgroupbyid;

import edu.pjwstk.api.groups.dto.GroupDto;
import edu.pjwstk.groups.model.Group;

public interface FindGroupByIdMapper {
    GroupDto toResponse(Group savedGroup);
}
