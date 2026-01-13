package pl.gamilife.group.model;

import org.instancio.Instancio;
import org.junit.jupiter.api.Test;
import pl.gamilife.group.enums.GroupTypeEnum;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowable;
import static org.instancio.Select.field;

class GroupTypeTest {

    @Test
    void shouldReturnCorrectEnum_whenIdIsValid() {
        // given
        GroupTypeEnum expectedEnum = GroupTypeEnum.values()[0];
        GroupType groupType = Instancio.of(GroupType.class)
                .set(field(GroupType::getId), expectedEnum.getId())
                .create();

        // when
        GroupTypeEnum result = groupType.toEnum();

        // then
        assertThat(result).isEqualTo(expectedEnum);
    }

    @Test
    void shouldThrowException_whenIdIsInvalid() {
        // given
        int invalidId = Integer.MAX_VALUE;
        GroupType groupType = Instancio.of(GroupType.class)
                .set(field(GroupType::getId), invalidId)
                .create();

        // when
        Throwable thrown = catchThrowable(groupType::toEnum);

        // then
        assertThat(thrown).isInstanceOf(RuntimeException.class);
    }

    @Test
    void shouldCreateGroupType_withBuilderAndFields() {
        // given
        String title = "Public Groups";
        List<Group> groups = Instancio.createList(Group.class);

        // when
        GroupType groupType = GroupType.builder()
                .title(title)
                .groups(groups)
                .build();

        // then
        assertThat(groupType).isNotNull();
        assertThat(groupType.getTitle()).isEqualTo(title);
        assertThat(groupType.getGroups()).isEqualTo(groups);
        assertThat(groupType.toString()).contains(title);
    }

    @Test
    void shouldSetAndGetProperties() {
        // given
        GroupType groupType = new GroupType();
        String title = "Private Groups";
        List<Group> groups = Instancio.createList(Group.class);

        // when
        groupType.setTitle(title);
        groupType.setGroups(groups);

        // then
        assertThat(groupType.getTitle()).isEqualTo(title);
        assertThat(groupType.getGroups()).isEqualTo(groups);
    }
}