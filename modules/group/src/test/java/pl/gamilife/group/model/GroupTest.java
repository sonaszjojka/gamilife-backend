package pl.gamilife.group.model;

import org.instancio.Instancio;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import pl.gamilife.group.enums.GroupTypeEnum;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;

import java.time.ZoneId;
import java.util.Collections;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowable;
import static org.instancio.Select.field;

class GroupTest {

    @Test
    void shouldCreateGroup_whenValidDataIsProvided() {
        // given
        String name = "Test Group";
        UUID adminId = UUID.randomUUID();
        String currency = "$";
        int limit = 10;
        ZoneId zoneId = ZoneId.of("Europe/Warsaw");
        GroupType groupType = Instancio.create(GroupType.class);

        // when
        Group group = Group.create(name, adminId, currency, limit, zoneId, groupType);

        // then
        assertThat(group).isNotNull();
        assertThat(group.getName()).isEqualTo(name);
        assertThat(group.getAdminId()).isEqualTo(adminId);
        assertThat(group.getCurrencySymbol()).isEqualTo('$');
        assertThat(group.getMembersLimit()).isEqualTo(limit);
        assertThat(group.getTimezone()).isEqualTo(zoneId.getId());
        assertThat(group.getType()).isEqualTo(groupType);
        assertThat(group.getTypeId()).isEqualTo(groupType.getId());
    }

    @Test
    void shouldReturnTrue_whenGroupIsFull() {
        // given
        int limit = 5;
        Set<GroupMember> members = IntStream.range(0, limit)
                .mapToObj(i -> Instancio.create(GroupMember.class))
                .collect(Collectors.toSet());

        Group group = Instancio.of(Group.class)
                .set(field(Group::getMembersLimit), limit)
                .set(field("activeMembers"), members)
                .create();

        // when
        boolean result = group.isFull();

        // then
        assertThat(result).isTrue();
    }

    @Test
    void shouldReturnFalse_whenGroupIsNotFull() {
        // given
        int limit = 10;
        Set<GroupMember> members = Collections.emptySet();

        Group group = Instancio.of(Group.class)
                .set(field(Group::getMembersLimit), limit)
                .set(field("activeMembers"), members)
                .create();

        // when
        boolean result = group.isFull();

        // then
        assertThat(result).isFalse();
    }

    @Test
    void shouldReturnTrue_whenUserIsAdmin() {
        // given
        UUID adminId = UUID.randomUUID();
        Group group = Instancio.of(Group.class)
                .set(field(Group::getAdminId), adminId)
                .create();

        // when
        boolean result = group.isUserAdmin(adminId);

        // then
        assertThat(result).isTrue();
    }

    @Test
    void shouldReturnFalse_whenUserIsNotAdmin() {
        // given
        UUID adminId = UUID.randomUUID();
        UUID otherUserId = UUID.randomUUID();
        Group group = Instancio.of(Group.class)
                .set(field(Group::getAdminId), adminId)
                .create();

        // when
        boolean result = group.isUserAdmin(otherUserId);

        // then
        assertThat(result).isFalse();
    }

    @Test
    void shouldReturnTrue_whenGroupIsOfType() {
        // given
        GroupTypeEnum expectedEnum = GroupTypeEnum.values()[0];

        GroupType groupType = Instancio.of(GroupType.class)
                .set(field(GroupType::getId), expectedEnum.getId())
                .create();

        Group group = Instancio.of(Group.class)
                .set(field(Group::getType), groupType)
                .create();

        // when
        boolean result = group.isOfType(expectedEnum);

        // then
        assertThat(result).isTrue();
    }

    @ParameterizedTest
    @ValueSource(strings = {"", "   "})
    void shouldThrowException_whenNameIsInvalid(String invalidName) {
        // given
        Group group = Instancio.create(Group.class);

        // when
        Throwable thrown = catchThrowable(() -> group.setName(invalidName));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Name cannot be null or empty");
    }

    @Test
    void shouldThrowException_whenNameIsNull() {
        // given
        Group group = Instancio.create(Group.class);

        // when
        Throwable thrown = catchThrowable(() -> group.setName(null));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Name cannot be null or empty");
    }

    @Test
    void shouldThrowException_whenNameIsTooLong() {
        // given
        String longName = "a".repeat(101);
        Group group = Instancio.create(Group.class);

        // when
        Throwable thrown = catchThrowable(() -> group.setName(longName));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Name cannot be longer than 100 characters");
    }

    @Test
    void shouldThrowException_whenAdminIdIsNull() {
        // given
        Group group = Instancio.create(Group.class);

        // when
        Throwable thrown = catchThrowable(() -> group.setAdminId(null));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Admin id cannot be null");
    }

    @Test
    void shouldThrowException_whenCurrencySymbolIsNull() {
        // given
        Group group = Instancio.create(Group.class);

        // when
        Throwable thrown = catchThrowable(() -> group.setCurrencySymbol(null));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Group currency symbol cannot be null");
    }

    @Test
    void shouldThrowException_whenCurrencySymbolIsInvalid() {
        // given
        String invalidSymbol = "ABC";
        Group group = Instancio.create(Group.class);

        // when
        Throwable thrown = catchThrowable(() -> group.setCurrencySymbol(invalidSymbol));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessageContaining("Group Currency Symbol must be one of");
    }

    @Test
    void shouldSetCurrencySymbol_whenSymbolIsValid() {
        // given
        String validSymbol = "€";
        Group group = Instancio.create(Group.class);

        // when
        group.setCurrencySymbol(validSymbol);

        // then
        assertThat(group.getCurrencySymbol()).isEqualTo('€');
    }

    @Test
    void shouldThrowException_whenMembersLimitIsZeroOrLess() {
        // given
        int invalidLimit = 0;
        Group group = Instancio.create(Group.class);

        // when
        Throwable thrown = catchThrowable(() -> group.setMembersLimit(invalidLimit));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Members limit cannot be less than or equal to 0");
    }

    @Test
    void shouldThrowException_whenMembersLimitIsTooHigh() {
        // given
        int invalidLimit = 101;
        Group group = Instancio.create(Group.class);

        // when
        Throwable thrown = catchThrowable(() -> group.setMembersLimit(invalidLimit));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Members limit cannot be greater than 100");
    }

    @Test
    void shouldThrowException_whenGroupTypeIsNull() {
        // given
        Group group = Instancio.create(Group.class);

        // when
        Throwable thrown = catchThrowable(() -> group.setGroupType(null));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Group type cannot be null");
    }

    @Test
    void shouldThrowException_whenTimezoneIsNull() {
        // given
        Group group = Instancio.create(Group.class);

        // when
        Throwable thrown = catchThrowable(() -> group.setTimezone(null));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessageContaining("Invalid timezone");
    }
}