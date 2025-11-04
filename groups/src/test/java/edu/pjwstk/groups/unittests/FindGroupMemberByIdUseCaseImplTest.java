package edu.pjwstk.groups.unittests;

import edu.pjwstk.common.groupsApi.dto.GroupMemberDto;
import edu.pjwstk.common.groupsApi.exception.GroupMemberNotFoundException;
import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupMember;
import edu.pjwstk.groups.repository.GroupMemberRepository;
import edu.pjwstk.groups.usecase.findgroupmemberbyid.FindGroupMemberByIdUseCaseImpl;
import edu.pjwstk.groups.usecase.findgroupmemberbyid.GroupMemberMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.Instant;
import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class FindGroupMemberByIdUseCaseImplTest {

    @Mock
    private GroupMemberRepository groupMemberRepository;

    @Mock
    private GroupMemberMapper groupMemberMapper;

    @InjectMocks
    private FindGroupMemberByIdUseCaseImpl findGroupMemberByIdUseCase;

    private UUID groupMemberId;
    private UUID groupId;
    private UUID userId;
    private GroupMember groupMember;
    private Group group;
    private GroupMemberDto response;

    @BeforeEach
    void setUp() {
        groupMemberId = UUID.randomUUID();
        groupId = UUID.randomUUID();
        userId = UUID.randomUUID();

        group = new Group();
        group.setGroupId(groupId);

        groupMember = new GroupMember();
        groupMember.setGroupMemberId(groupMemberId);
        groupMember.setMemberGroup(group);
        groupMember.setUserId(userId);
        groupMember.setJoinedAt(Instant.now().minusSeconds(3600));
        groupMember.setGroupMoney(100);
        groupMember.setTotalEarnedMoney(500);

        GroupMemberDto.GroupDto groupDto = new GroupMemberDto.GroupDto(groupId);

        response = GroupMemberDto.builder()
                .groupMemberId(groupMemberId)
                .memberGroup(groupDto)
                .userId(userId)
                .joinedAt(groupMember.getJoinedAt())
                .leftAt(null)
                .groupMoney(100)
                .totalEarnedMoney(500)
                .build();
    }

    @Test
    void shouldSuccessfullyFindGroupMember_whenGroupMemberExists() {
        // Given
        when(groupMemberRepository.findById(groupMemberId)).thenReturn(Optional.of(groupMember));
        when(groupMemberMapper.toResponse(groupMember)).thenReturn(response);

        // When
        GroupMemberDto result = findGroupMemberByIdUseCase.execute(groupMemberId);

        // Then
        assertNotNull(result);
        assertEquals(response, result);
        assertEquals(groupMemberId, result.groupMemberId());
        assertEquals(userId, result.userId());
        assertEquals(groupId, result.memberGroup().groupId());
        assertEquals(100, result.groupMoney());
        assertEquals(500, result.totalEarnedMoney());
        assertNull(result.leftAt());

        verify(groupMemberRepository).findById(groupMemberId);
        verify(groupMemberMapper).toResponse(groupMember);
    }

    @Test
    void shouldThrowGroupMemberNotFoundException_whenGroupMemberDoesNotExist() {
        // Given
        when(groupMemberRepository.findById(groupMemberId)).thenReturn(Optional.empty());

        // When
        GroupMemberNotFoundException exception = assertThrows(
                GroupMemberNotFoundException.class,
                () -> findGroupMemberByIdUseCase.execute(groupMemberId)
        );

        // Then
        assertEquals("Group member with id: " + groupMemberId + " not found", exception.getMessage());
        verify(groupMemberRepository).findById(groupMemberId);
        verify(groupMemberMapper, never()).toResponse(any());
    }
}