package edu.pjwstk.groups.service;

import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.model.GroupMember;

import java.util.UUID;

public interface GroupMemberService {
    GroupMember createGroupMember(Group group, UUID userId);
}
