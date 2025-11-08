package edu.pjwstk.groups.usecase.editgroupinvitationstatus;

import edu.pjwstk.groups.model.GroupInvitation;
import edu.pjwstk.groups.usecase.creategroupmember.CreateGroupMemberResponse;

public interface EditGroupInvitationStatusMapper {
    EditGroupInvitationStatusResponse toResponse(GroupInvitation savedGroupInvitation,
                                                 CreateGroupMemberResponse groupMember);
}
