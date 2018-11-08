-- gen triggers - start




drop function public.process_audit_user() cascade;
create or replace function public.process_audit_user()
 returns trigger
 language plpgsql
as $function$
   begin
       if to_regclass('user_history') is not null then
           if (TG_OP = 'UPDATE' or TG_OP = 'INSERT') then
                insert into user_history
                       (id, ident, password, email, is_admin, is_editor, is_reviewer, is_author, version, created_at, created_by, updated_at, updated_by)
                       values
                       (new.id, new.ident, new.password, new.email, new.is_admin, new.is_editor, new.is_reviewer, new.is_author, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by);
                return new;
            end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_user after insert or update on public.user for each row execute procedure public.process_audit_user();



drop function public.process_audit_config() cascade;
create or replace function public.process_audit_config()
 returns trigger
 language plpgsql
as $function$
   begin
       if to_regclass('config_history') is not null then
           if (TG_OP = 'UPDATE' or TG_OP = 'INSERT') then
                insert into config_history
                       (id, code, string_value, int_value, double_value, bool_value, version, created_at, created_by, updated_at, updated_by)
                       values
                       (new.id, new.code, new.string_value, new.int_value, new.double_value, new.bool_value, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by);
                return new;
            end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_config after insert or update on public.config for each row execute procedure public.process_audit_config();




drop function public.process_audit_submission() cascade;
create or replace function public.process_audit_submission()
 returns trigger
 language plpgsql
as $function$
   begin
       if to_regclass('submission_history') is not null then
           if (TG_OP = 'UPDATE' or TG_OP = 'INSERT') then
                insert into submission_history
                       (id, headline, subline, version, created_at, created_by, updated_at, updated_by)
                       values
                       (new.id, new.headline, new.subline, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by);
                return new;
            end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_submission after insert or update on public.submission for each row execute procedure public.process_audit_submission();

-- gen triggers - end
