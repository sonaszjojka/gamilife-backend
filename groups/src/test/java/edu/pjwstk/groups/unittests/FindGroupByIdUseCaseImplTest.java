package edu.pjwstk.groups.unittests;

import edu.pjwstk.common.groupsApi.dto.GroupDto;
import edu.pjwstk.common.groupsApi.exception.GroupNotFoundException;
import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupType;
import edu.pjwstk.groups.repository.GroupRepository;
import edu.pjwstk.groups.usecase.findgroupbyid.FindGroupByIdMapper;
import edu.pjwstk.groups.usecase.findgroupbyid.FindGroupByIdUseCaseImpl;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class FindGroupByIdUseCaseImplTest {

    @Mock
    private GroupRepository groupRepository;

    @Mock
    private FindGroupByIdMapper findGroupByIdMapper;

    @InjectMocks
    private FindGroupByIdUseCaseImpl findGroupByIdUseCase;

    private UUID groupId;
    private UUID adminId;
    private Group group;
    private GroupType groupType;
    private GroupDto response;

    @BeforeEach
    void setUp() {
        groupId = UUID.randomUUID();
        adminId = UUID.randomUUID();

        groupType = new GroupType();
        groupType.setGroupTypeId(1);
        groupType.setTitle("Family");

        group = new Group();
        group.setGroupId(groupId);
        group.setJoinCode("ABC123");
        group.setAdminId(adminId);
        group.setGroupCurrencySymbol('$');
        group.setMembersLimit(10);
        group.setGroupType(groupType);

        GroupDto.GroupTypeDto groupTypeDto = GroupDto.GroupTypeDto.builder()
                .groupTypeId(1)
                .title("Family")
                .build();

        response = GroupDto.builder()
                .groupId(groupId)
                .joinCode("ABC123")
                .adminId(adminId)
                .groupCurrencySymbol('$')
                .membersLimit(10)
                .groupType(groupTypeDto)
                .build();
    }

    @Test
    void shouldSuccessfullyFindGroup_whenGroupExists() {
        // Given
        when(groupRepository.findById(groupId)).thenReturn(Optional.of(group));
        when(findGroupByIdMapper.toResponse(group)).thenReturn(response);

        // When
        GroupDto result = findGroupByIdUseCase.execute(groupId);

        // Then
        assertNotNull(result);
        assertEquals(response, result);
        assertEquals(groupId, result.groupId());
        assertEquals("ABC123", result.joinCode());
        assertEquals(adminId, result.adminId());
        assertEquals('$', result.groupCurrencySymbol());
        assertEquals(10, result.membersLimit());
        assertNotNull(result.groupType());
        assertEquals(1, result.groupType().groupTypeId());
        assertEquals("Family", result.groupType().title());

        verify(groupRepository).findById(groupId);
        verify(findGroupByIdMapper).toResponse(group);
    }

    @Test
    void shouldThrowGroupNotFoundException_whenGroupDoesNotExist() {
        // Given
        when(groupRepository.findById(groupId)).thenReturn(Optional.empty());

        // When
        GroupNotFoundException exception = assertThrows(
                GroupNotFoundException.class,
                () -> findGroupByIdUseCase.execute(groupId)
        );

        // Then
        assertEquals("Group with id:" + groupId + " not found!", exception.getMessage());
        verify(groupRepository).findById(groupId);
        verify(findGroupByIdMapper, never()).toResponse(any());
    }
}