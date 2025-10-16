package edu.pjwstk.groups.usecase.editgroupinvitationstatus;

import edu.pjwstk.groups.entity.GroupInvitation;

public interface EditGroupInvitationStatusMapper {
    EditGroupInvitationStatusResponse toResponse(GroupInvitation savedGroupInvitation);
}
