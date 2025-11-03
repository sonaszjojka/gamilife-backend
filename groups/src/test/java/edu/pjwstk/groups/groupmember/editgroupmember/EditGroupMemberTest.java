package edu.pjwstk.groups.groupmember.editgroupmember;

import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupMember;
import edu.pjwstk.groups.entity.GroupType;
import edu.pjwstk.groups.repository.GroupMemberRepository;
import edu.pjwstk.groups.usecase.editgroupmember.EditGroupMemberRequest;
import edu.pjwstk.groups.usecase.editgroupmember.EditGroupMemberResponse;
import edu.pjwstk.groups.usecase.editgroupmember.EditGroupMemberUseCaseImpl;
import edu.pjwstk.groups.usecase.editgroupmember.EditGroupMemberMapper;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
public class EditGroupMemberTest {

    @Mock
    private GroupMemberRepository groupMemberRepository;


    @Mock
    private EditGroupMemberMapper editGroupMemberMapper;

    @InjectMocks
    private EditGroupMemberUseCaseImpl editGroupMemberUseCase;



    @Test
    void shouldEditGroupMemberSuccessfully() {
        UUID groupMemberId = UUID.randomUUID();
        UUID groupId = UUID.randomUUID();
        UUID userId = UUID.randomUUID();
        UUID adminId = UUID.randomUUID();

        GroupType groupType = GroupType.builder()
                .groupTypeId(1)
                .title("Open")
                .build();

        Group group = Group.builder()
                .groupId(groupId)
                .joinCode("ABC123XYZ456")
                .adminId(adminId)
                .groupCurrencySymbol('$')
                .membersLimit(10)
                .groupType(groupType)
                .build();

        GroupMember adminMember = GroupMember.builder()
                .groupMemberId(adminId)
                .userId(adminId)
                .memberGroup(group)
                .joinedAt(Instant.now().minusSeconds(30 * 24 * 60 * 60))
                .groupMoney(1000)
                .totalEarnedMoney(5000)
                .leftAt(null)
                .build();

        GroupMember regularMember = GroupMember.builder()
                .groupMemberId(groupMemberId)
                .userId(userId)
                .memberGroup(group)
                .joinedAt(Instant.now().minusSeconds(10 * 24 * 60 * 60))
                .groupMoney(500)
                .totalEarnedMoney(2000)
                .leftAt(null)
                .build();

        List<GroupMember> groupMembers = new ArrayList<>();
        groupMembers.add(adminMember);
        groupMembers.add(regularMember);
        group.setGroupMembers(groupMembers);

        EditGroupMemberRequest request = EditGroupMemberRequest.builder()
                .groupMoney(1000)
                .totalEarnedMoney(2500)
                .build();

        GroupMember updatedMember = GroupMember.builder()
                .groupMemberId(groupMemberId)
                .userId(userId)
                .memberGroup(group)
                .joinedAt(Instant.now().minusSeconds(10 * 24 * 60 * 60))
                .groupMoney(1000)
                .totalEarnedMoney(2500)
                .leftAt(null)
                .build();

        EditGroupMemberResponse.GroupDto groupDto = EditGroupMemberResponse.GroupDto.builder()
                .groupId(groupId)
                .adminId(adminId)
                .build();

        EditGroupMemberResponse expectedResponse = EditGroupMemberResponse.builder()
                .groupMemberId(groupMemberId)
                .memberGroup(groupDto)
                .userId(userId)
                .joinedAt(regularMember.getJoinedAt())
                .leftAt(null)
                .groupMoney(1000)
                .totalEarnedMoney(2500)
                .build();

        when(groupMemberRepository.findById(groupMemberId)).thenReturn(Optional.of(regularMember));
        when(groupMemberRepository.save(any(GroupMember.class))).thenReturn(updatedMember);
        when(editGroupMemberMapper.toResponse(any(GroupMember.class))).thenReturn(expectedResponse);

       EditGroupMemberResponse result = editGroupMemberUseCase.execute(groupMemberId, request);

        assertThat(result).isNotNull();
        assertThat(result.groupMemberId()).isEqualTo(groupMemberId);
        assertThat(result.userId()).isEqualTo(userId);
        assertThat(result.memberGroup()).isNotNull();
        assertThat(result.memberGroup().groupId()).isEqualTo(groupId);
        assertThat(result.memberGroup().adminId()).isEqualTo(adminId);
        assertThat(result.leftAt()).isNull();
        assertThat(result.joinedAt()).isNotNull();
        assertThat(result.groupMoney()).isEqualTo(1000);
        assertThat(result.totalEarnedMoney()).isEqualTo(2500);

        ArgumentCaptor<GroupMember> groupMemberCaptor = ArgumentCaptor.forClass(GroupMember.class);
        verify(groupMemberRepository).save(groupMemberCaptor.capture());

        GroupMember capturedMember = groupMemberCaptor.getValue();
        assertThat(capturedMember.getLeftAt()).isNull();
        assertThat(capturedMember.getGroupMemberId()).isEqualTo(groupMemberId);
        assertThat(capturedMember.getGroupMoney()).isEqualTo(1000);
        assertThat(capturedMember.getTotalEarnedMoney()).isEqualTo(2500);

        verify(groupMemberRepository).findById(groupMemberId);
        verify(groupMemberRepository).save(any(GroupMember.class));
        verify(editGroupMemberMapper).toResponse(updatedMember);
    }


}





