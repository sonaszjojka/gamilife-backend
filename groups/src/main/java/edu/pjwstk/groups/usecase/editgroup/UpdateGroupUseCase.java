package edu.pjwstk.groups.usecase.editgroup;

import java.util.UUID;

public interface UpdateGroupUseCase {
    UpdateGroupResponse execute(UpdateGroupRequest request, UUID groupId);
}
