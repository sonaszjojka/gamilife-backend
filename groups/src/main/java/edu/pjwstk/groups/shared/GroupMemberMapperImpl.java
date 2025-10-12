package edu.pjwstk.groups.shared;

import edu.pjwstk.groups.domain.GroupMember;
import org.springframework.stereotype.Component;

@Component
public class GroupMemberMapperImpl implements GroupMemberMapper {

    @Override
    public GroupMemberDto toResponse(GroupMember groupMember) {
        return GroupMemberDto.builder()
                .groupMemberId(groupMember.getGroupMemberId())
                .memberGroup(new GroupMemberDto.GroupDto(groupMember.getMemberGroup().getGroupId()))
                .userId(groupMember.getUserId())
                .joinedAt(groupMember.getJoinedAt())
                .leftAt(groupMember.getLeftAt())
                .groupMoney(groupMember.getGroupMoney())
                .totalEarnedMoney(groupMember.getTotalEarnedMoney())
                .build();
    }
}
