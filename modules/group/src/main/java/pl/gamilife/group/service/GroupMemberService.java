package pl.gamilife.group.service;

import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupMember;

import java.util.UUID;

public interface GroupMemberService {
    GroupMember createGroupMember(Group group, UUID userId);
}
