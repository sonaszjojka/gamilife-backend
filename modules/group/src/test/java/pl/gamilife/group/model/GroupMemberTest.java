package pl.gamilife.group.model;

import org.instancio.Instancio;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import pl.gamilife.group.exception.domain.NotEnoughGroupMoneyException;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;

import java.time.Instant;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowable;
import static org.instancio.Select.field;

class GroupMemberTest {

    @Test
    void shouldCreateGroupMember_whenValidDataIsProvided() {
        // given
        Group group = Instancio.create(Group.class);
        UUID userId = UUID.randomUUID();

        // when
        GroupMember member = GroupMember.create(group, userId);

        // then
        assertThat(member).isNotNull();
        assertThat(member.getGroup()).isEqualTo(group);
        assertThat(member.getGroupId()).isEqualTo(group.getId());
        assertThat(member.getUserId()).isEqualTo(userId);
        assertThat(member.getGroupMoney()).isZero();
        assertThat(member.getTotalEarnedMoney()).isZero();
        assertThat(member.joinedAt).isNotNull();
        assertThat(member.isActive()).isTrue();
    }

    @Test
    void shouldThrowException_whenGroupIsNull() {
        // given
        UUID userId = UUID.randomUUID();

        // when
        Throwable thrown = catchThrowable(() -> GroupMember.create(null, userId));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Group cannot be null");
    }

    @Test
    void shouldThrowException_whenUserIdIsNull() {
        // given
        Group group = Instancio.create(Group.class);

        // when
        Throwable thrown = catchThrowable(() -> GroupMember.create(group, null));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("User id cannot be null");
    }

    @Test
    void shouldLeaveGroup_whenMemberIsActive() {
        // given
        GroupMember member = Instancio.of(GroupMember.class)
                .set(field("leftAt"), null)
                .create();

        // when
        member.leave();

        // then
        assertThat(member.isActive()).isFalse();
        assertThat(member.leftAt).isNotNull();
    }

    @Test
    void shouldThrowException_whenLeavingAlreadyLeftGroup() {
        // given
        GroupMember member = Instancio.of(GroupMember.class)
                .set(field("leftAt"), Instant.now())
                .create();

        // when
        Throwable thrown = catchThrowable(member::leave);

        // then
        assertThat(thrown)
                .isInstanceOf(IllegalStateException.class)
                .hasMessage("User has already left the group");
    }

    @Test
    void shouldRejoinGroup_whenMemberHasLeft() {
        // given
        Instant oldJoinedAt = Instant.now().minusSeconds(1000);
        GroupMember member = Instancio.of(GroupMember.class)
                .set(field("leftAt"), Instant.now())
                .set(field("joinedAt"), oldJoinedAt)
                .create();

        // when
        member.rejoin();

        // then
        assertThat(member.isActive()).isTrue();
        assertThat(member.leftAt).isNull();
        assertThat(member.joinedAt).isAfter(oldJoinedAt);
    }

    @Test
    void shouldThrowException_whenRejoiningActiveGroup() {
        // given
        GroupMember member = Instancio.of(GroupMember.class)
                .set(field("leftAt"), null)
                .create();

        // when
        Throwable thrown = catchThrowable(member::rejoin);

        // then
        assertThat(thrown)
                .isInstanceOf(IllegalStateException.class)
                .hasMessage("User has not left the group");
    }

    @Test
    void shouldReturnTrue_whenUserIdMatches() {
        // given
        UUID userId = UUID.randomUUID();
        GroupMember member = Instancio.of(GroupMember.class)
                .set(field(GroupMember::getUserId), userId)
                .create();

        // when
        boolean result = member.isUser(userId);

        // then
        assertThat(result).isTrue();
    }

    @Test
    void shouldReturnFalse_whenUserIdDoesNotMatch() {
        // given
        UUID userId = UUID.randomUUID();
        GroupMember member = Instancio.of(GroupMember.class)
                .set(field(GroupMember::getUserId), UUID.randomUUID())
                .create();

        // when
        boolean result = member.isUser(userId);

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
        GroupMember member = Instancio.of(GroupMember.class)
                .set(field(GroupMember::getGroup), group)
                .set(field(GroupMember::getUserId), adminId)
                .create();

        // when
        boolean result = member.isAdmin();

        // then
        assertThat(result).isTrue();
    }

    @Test
    void shouldReturnFalse_whenUserIsNotAdmin() {
        // given
        UUID adminId = UUID.randomUUID();
        UUID userId = UUID.randomUUID(); // Inne ID

        Group group = Instancio.of(Group.class)
                .set(field(Group::getAdminId), adminId)
                .create();

        GroupMember member = Instancio.of(GroupMember.class)
                .set(field(GroupMember::getGroup), group)
                .set(field(GroupMember::getUserId), userId)
                .create();

        // when
        boolean result = member.isAdmin();

        // then
        assertThat(result).isFalse();
    }

    @Test
    void shouldGainMoney_whenAmountIsValid() {
        // given
        int initialMoney = 100;
        int initialTotal = 200;
        int amount = 50;

        GroupMember member = Instancio.of(GroupMember.class)
                .set(field(GroupMember::getGroupMoney), initialMoney)
                .set(field(GroupMember::getTotalEarnedMoney), initialTotal)
                .create();

        // when
        member.gainMoney(amount);

        // then
        assertThat(member.getGroupMoney()).isEqualTo(initialMoney + amount);
        assertThat(member.getTotalEarnedMoney()).isEqualTo(initialTotal + amount);
    }

    @ParameterizedTest
    @ValueSource(ints = {0, -1, -100})
    void shouldThrowException_whenGainAmountIsInvalid(int invalidAmount) {
        // given
        GroupMember member = Instancio.create(GroupMember.class);

        // when
        Throwable thrown = catchThrowable(() -> member.gainMoney(invalidAmount));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Amount cannot be less than or equal to 0");
    }

    @Test
    void shouldUseMoney_whenAmountIsValidAndBalanceSufficient() {
        // given
        int initialMoney = 100;
        int amount = 50;

        GroupMember member = Instancio.of(GroupMember.class)
                .set(field(GroupMember::getGroupMoney), initialMoney)
                .create();

        // when
        member.useMoney(amount);

        // then
        assertThat(member.getGroupMoney()).isEqualTo(initialMoney - amount);
    }

    @ParameterizedTest
    @ValueSource(ints = {0, -1})
    void shouldThrowException_whenUseAmountIsInvalid(int invalidAmount) {
        // given
        GroupMember member = Instancio.create(GroupMember.class);

        // when
        Throwable thrown = catchThrowable(() -> member.useMoney(invalidAmount));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Amount cannot be less than or equal to 0");
    }

    @Test
    void shouldThrowException_whenUseMoneyInsufficientFunds() {
        // given
        int initialMoney = 10;
        int amount = 50;

        GroupMember member = Instancio.of(GroupMember.class)
                .set(field(GroupMember::getGroupMoney), initialMoney)
                .create();

        // when
        Throwable thrown = catchThrowable(() -> member.useMoney(amount));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Not enough money to use");
    }

    @Test
    void shouldPayMoney_whenAmountIsValidAndBalanceSufficient() {
        // given
        int initialMoney = 100;
        int amount = 50;

        GroupMember member = Instancio.of(GroupMember.class)
                .set(field(GroupMember::getGroupMoney), initialMoney)
                .create();

        // when
        member.payMoney(amount);

        // then
        assertThat(member.getGroupMoney()).isEqualTo(initialMoney - amount);
    }

    @ParameterizedTest
    @ValueSource(ints = {0, -1})
    void shouldThrowException_whenPayMoneyAmountIsInvalid(int invalidAmount) {
        // given
        GroupMember member = Instancio.create(GroupMember.class);

        // when
        Throwable thrown = catchThrowable(() -> member.payMoney(invalidAmount));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Amount cannot be less than or equal to 0");
    }

    @Test
    void shouldThrowException_whenPayMoneyInsufficientFunds() {
        // given
        int initialMoney = 10;
        int amount = 50;

        GroupMember member = Instancio.of(GroupMember.class)
                .set(field(GroupMember::getGroupMoney), initialMoney)
                .create();

        // when
        Throwable thrown = catchThrowable(() -> member.payMoney(amount));

        // then
        assertThat(thrown)
                .isInstanceOf(NotEnoughGroupMoneyException.class)
                .hasMessage("Not enough money to pay");
    }

    @Test
    void shouldSetGroupMoneyAndRecalculateTotalEarned() {
        // given
        int initialGroupMoney = 100;
        int initialTotalEarned = 500;
        int newGroupMoney = 150;
        GroupMember member = Instancio.of(GroupMember.class)
                .set(field(GroupMember::getGroupMoney), initialGroupMoney)
                .set(field(GroupMember::getTotalEarnedMoney), initialTotalEarned)
                .create();

        // when
        member.setGroupMoney(newGroupMoney);

        // then
        assertThat(member.getGroupMoney()).isEqualTo(newGroupMoney);
        assertThat(member.getTotalEarnedMoney()).isEqualTo(550);
    }

    @Test
    void shouldThrowException_whenSetGroupMoneyIsNull() {
        // given
        GroupMember member = Instancio.create(GroupMember.class);

        // when
        Throwable thrown = catchThrowable(() -> member.setGroupMoney(null));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Group money cannot be null");
    }

    @Test
    void shouldThrowException_whenSetGroupMoneyIsNegative() {
        // given
        GroupMember member = Instancio.create(GroupMember.class);

        // when
        Throwable thrown = catchThrowable(() -> member.setGroupMoney(-1));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Group money cannot be negative");
    }
}